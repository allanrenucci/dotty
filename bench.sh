#!/bin/sh

#DOTTY=~/dotty/dotty.jar
DOTTY=~/dotty/target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar
DOTTY_INTERFACES=~/dotty/interfaces/target/dotty-interfaces-0.1-SNAPSHOT.jar
SCALA_LIBRARY=~/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.5.jar
SCALA_REFLECT=~/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.5.jar
JLINE=~/.ivy2/cache/jline/jline/jars/jline-2.12.jar

CLASSPATH="$DOTTY":"$SCALA_LIBRARY":"$SCALA_REFLECT":"$DOTTY_INTERFACES":"$JLINE"

MAIN=dotty.tools.dotc.Main

time java -Xbootclasspath/a:$CLASSPATH $MAIN