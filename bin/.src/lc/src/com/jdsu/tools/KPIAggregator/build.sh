CLASSPATH=/u01/app/oracle/product/9.2.0/jdbc/lib/classes12.jar:~/cbt_ops/lib/commons-logging.jar:~/cbt_ops/lib/spring.jar:~/cbt_ops/classes:.
CLASSDIR=~/cbt_ops/classes
javac -d ${CLASSDIR} KPIAggregator.java
javac -d ${CLASSDIR} KPIAggregatorTest.java
