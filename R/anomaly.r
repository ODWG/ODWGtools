anomaly_ets = function(d) {

#d = as_tsibble(data.frame(x = nsl$DateTime, y = nsl$Value), index = x) %>%
#  filter(x < as_datetime("2017-10-01", "US/Pacific"))
#
#d["gap"] = rtqc_gap(d$x, "15 min")
#d[3460:3470,]
#
#m = ETS(y ~ x),
#
#d %>%
#   model(m)
#     
#     ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>% 
#   forecast(h = "3 years")
}