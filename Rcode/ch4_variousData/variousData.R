rm(list=ls())

graph_new = function() {
  os = Sys.info()["sysname"]
  if (os == 'Windows') {
    windows()
  } else if (os == 'Darwin') {
    quartz()
  } else if (os == 'Linux') {
    x11()
  }
}
