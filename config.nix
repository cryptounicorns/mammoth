# FIXME: Time in nanoseconds will be truncated little bit
#
#   1519397252459093504 - 1519397252459093536
# = -32 ns
# This is because of lua internal representation of numbers,
# it is float64, but for nanoseconds we need uint64,
# floats may be inexact, so we lost our 32 ns.
# I don't have clear idea about a fix ATM

{
  Logger = {
    Formatter = "json";
    Level = "info";
  };
  HTTP = {
    Addr = "0.0.0.0:8080";
  };
  Router =
    let
      database = "http://127.0.0.1:8086";
      tickersParameters = extra:
        [
          { Name = "metric";       Type  = "string";  }
          { Name = "from";         Type  = "int64";   }
          { Name = "to";           Type  = "int64";   }
          { Name = "market";       Type  = "string";  }
          { Name = "symbolPair";   Type  = "string";  }
        ] ++ extra;
      tickersValidator = extra:
        {
          Type = "lua";
          Lua = {
            FunctionName = "validate";
            Code =
              ''
                local math      = require("gomath")
                local time      = require("gotime")
                local validator = require("govalidator")

                function validate(p)
                  if string.len(p["market"]) == 0                                          then return "market should not be empty" end
                  if not validator.IsIn(p["metric"], "last", "high", "low")                then return "metric is not valid" end
                  if math.Abs(p["from"] - p["to"]) > (time.Hour * 24 * 7)                  then return "from and to fields represents too wide timerange" end
                  if not validator.IsAlphanumeric(p["market"])                             then return "market should be alphanumeric" end
                  if not validator.Matches(p["symbolPair"], "^[A-Za-z0-9]+-[A-Za-z0-9]+$") then return "symbolPair is invalid" end

                  ${extra}

                  return nil
                end
              '';
          };
        };
      tickersTransformer = extra:
        {
          Type = "lua";
          Lua = {
            FunctionName = "transform";
            Code =
              ''
                local strings = require("gostrings")

                function transform(p)
                  p["symbolPair"] = strings.ToUpper(p["symbolPair"])

                  ${extra}

                  return p
                end
              '';
          };
        };
    in [
      {
        Path = "/api/v1/tickers";
        Method = "get";
        Description = {
          Short = ''
            Produces time grouped means on specified ticker metric.
          '';
        };

        Handler = {
          Parameters = tickersParameters
            [
              {
                Name = "resolution";
                Type = "string";
              }
            ];

          Validator = tickersValidator
            ''
              local resolution
              local err

              resolution, err = time.ParseDuration(p["resolution"])

              if err                    then return "invalid resolution syntax" end
              if resolution < time.Hour then return "resolution should be greater or equal 1 hour" end
            '';

          Transformer = tickersTransformer "";

          Database = {
            Type = "influxdb";
            Influxdb = {
              Database = "gluttony";
              Client = {
                Addr = database;
              };
              Precision = "nanosecond";
            };

            Query = ''
              select mean({{ .Escape .Parameters.metric }})
                as {{ .Escape .Parameters.metric }}
              from tickers
              where
                time >= {{ printf "%.0f" .Parameters.from }}
                and time <= {{ printf "%.0f" .Parameters.to }}
                and market = '{{ .Escape .Parameters.market }}'
                and symbolPair = '{{ .Escape .Parameters.symbolPair }}'
              group by time({{ .Escape .Parameters.resolution }})
            '';
          };
          Response = {
            Format = "json";
            Builder = {
              Type = "columns";
              Columns = {
                Database = "influxdb";
              };
            };
          };
        };
      }
      {
        Path = "/api/v1/changes";
        Method = "get";
        Description = {
          Short = ''
            Calculates percent change of the metric.
          '';
        };

        Handler = {
          Parameters = tickersParameters [];
          Validator = tickersValidator "";
          Transformer = tickersTransformer "";

          Database = {
            Type = "influxdb";
            Influxdb = {
              Database = "gluttony";
              Client = {
                Addr = database;
              };
              Precision = "nanosecond";
            };

            Query = ''
              select
                100 - (first({{ .Escape .Parameters.metric }}) / last({{ .Escape .Parameters.metric }})) * 100
                  as {{ .Escape .Parameters.metric }}
              from tickers
              where
                time >= {{ printf "%.0f" .Parameters.from }}
                and time <= {{ printf "%.0f" .Parameters.to }}
                and market = '{{ .Escape .Parameters.market }}'
                and symbolPair = '{{ .Escape .Parameters.symbolPair }}'
            '';
          };
          Response = {
            Format = "json";
            Builder = {
              Type = "columns";
              Columns = {
                Database = "influxdb";
              };
            };
          };
        };
      }
    ];
}
