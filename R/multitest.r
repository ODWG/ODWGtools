#' @importFrom yaml read_yaml
multitest = function(d, specification.file) {

  test.specs = read_yaml(specification.file)

  parameter.field = test.specs$data_specifications$parameter_field
  datetime.field = test.specs$data_specifications$datetime_field
  station.field = test.specs$data_specifications$station_field

  parameters = names(test.specs$parameters)

  for (param in parameters) {
    pd = filter(d, .data[[parameter.field]] == param)
    rtqc.tests = names(test.specs$parameters[[param]])
    for (test in rtqc.tests) {
      testfun = match.fun(test)
      test.args = test.specs$parameters[[param]][[test]]
      
    }

    p.datetime = select(d, .data[[datetime.field]])
    p.station = select(d, .data[[station.field]])
    


  }



}