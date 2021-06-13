# -*- mode: shell-script;-*-

##
#  Hadoop environment 
#

#set -x


SPARK_DEFAULT=/opt/srv/apache/spark

if [ -d "$SPARK_DEFAULT" ]; then


        case "$HADOOP_VERSION"   in
                2.6)
                        SPARK_HOME=/opt/srv/apache/spark1
                        SPARK_MAJOR_VERSION=1
                        ;;
                2.7)
                        SPARK_HOME=/opt/srv/apache/spark2
                        SPARK_MAJOR_VERSION=2
                        ;;
                *)
                        SPARK_HOME=/opt/srv/apache/spark2
                        SPARK_MAJOR_VERSION=2
                        ;;
        esac
        export SPARK_HOME
        export SPARK_MAJOR_VERSION
    
    
fi



#set +x
