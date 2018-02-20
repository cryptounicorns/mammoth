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
          { Name = "from";         Type  = "float64"; }
          { Name = "to";           Type  = "float64"; }
          { Name = "market";       Type  = "string";  }
          { Name = "currencyPair"; Type  = "string";  }
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
                  if not validator.IsIn(p["metric"], "last", "high", "low")      then return "metric is not valid" end
                  if math.Abs(p["from"] - p["to"]) > (time.Hour * 24 * 7)        then return "from & to represents too wide timerange" end
                  if not validator.IsAlphanumeric(p["market"])                   then return "market should be alphanumeric" end
                  if not validator.Matches(p["currencyPair"], "^[A-Z]+-[A-Z]+$") then return "currencyPair is invalid" end

                  ${extra}

                  return nil
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
          Format = "json";
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

          Database = {
            Type = "influxdb";
            Influxdb = {
              Database = "gluttony";
              Client = {
                Addr = database;
              };
              Precision = "nanosecond";
              Query = ''
                select mean($metric)
                from tickers
                where
                  time >= $from
                  and time <= $to
                  and market = $market
                  and currencyPair = $currencyPair
                group by time($resolution)
              '';
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
          Format = "json";
          Parameters = tickersParameters [];
          Validator = tickersValidator "";

          Database = {
            Type = "influxdb";
            Influxdb = {
              Database = "gluttony";
              Client = {
                Addr = database;
              };
              Precision = "nanosecond";
              Query = ''
                select
                  100 - (first($metric) / last($metric)) * 100 as $metric
                from tickers
                where
                  time >= $from
                  and time <= $to
                  and market = $market
                  and currencyPair = $currencyPair
              '';
            };
          };
        };
      }
    ];
}
