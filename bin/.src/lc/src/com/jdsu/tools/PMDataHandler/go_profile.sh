CLASSPATH=/u01/app/oracle/product/9.2.0/jdbc/lib/classes12.jar:/ada/tdce/java:.:~/cbt_ops/src

#java -cp ${CLASSPATH} PMDataHandler.PMDataHandler

OPTIT_HOME="/home/lamberk/optimizeit/OptimizeitSuite55"
OPTIT_PORT=4444
ORACLE_JDBC_CLASSPATH=/u01/app/oracle/product/9.2.0/jdbc/lib/classes12.jar
ORACLE_LD_LIB_PATH=/u01/app/oracle/product/9.2.0/lib32
NETO_CLASSPATH=/ada/tdce/java:/ada/tdce/sys/cbt/umove/
export OPTIT_HOME
OPTIT_CLASSPATH="${CLASSPATH}:${OPTIT_HOME}/lib/optit.jar:${ORACLE_JDBC_CLASSPATH}:${NETO_CLASSPATH}"
LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${OPTIT_HOME}/lib:${ORACLE_LD_LIB_PATH}"
export LD_LIBRARY_PATH
OPTIT="-Xbootclasspath/a:${OPTIT_HOME}/lib/oibcp.jar -Xrunpri -Xint intuitive.audit.Audit -port ${OPTIT_PORT}"
OPT_OPS=-Djava.library.path=${LD_LIBRARY_PATH}


echo "STARTING OPTIT"
CMD="java -cp ${OPTIT_CLASSPATH} ${OPTIT} com.jdsu.tools.PMDataHandler.PMDataHandler"
echo $CMD
$CMD
