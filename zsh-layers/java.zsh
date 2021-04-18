###############################################################################
# Java & Maven
#
export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_11_HOME=$(/usr/libexec/java_home -v11)

export MAVEN_OPTS="-Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true"

use-java8() {
  JAVA_HOME="${JAVA_8_HOME}"
  export PATH="${JAVA_HOME}/bin:${PATH}"
}

use-java11() {
  JAVA_HOME="${JAVA_11_HOME}"
  export PATH="${JAVA_HOME}/bin:${PATH}"
}

use-java11


export GROOVY_HOME=/usr/local/opt/groovy/libexec
