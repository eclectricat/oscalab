run / javaOptions += "-Djava.library.path=./lib/"
run / javaOptions += "-Xverify:none"


run / fork := true

libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.6"
