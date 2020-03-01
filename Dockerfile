FROM centos:centos8
ENV SCALA_VERSION 2.13.1
ENV SBT_VERSION 0.13.17

RUN yum install -y epel-release
RUN yum install -y unzip
RUN yum install -y lsof
RUN yum update -y && yum install -y wget

# INSTALL JAVA
RUN yum install -y java-1.8.0-openjdk

# INSTALL SBT
RUN wget http://dl.bintray.com/sbt/rpm/sbt-${SBT_VERSION}.rpm
RUN yum install -y sbt-${SBT_VERSION}.rpm

RUN wget -O /usr/local/bin/sbt-launch.jar http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/$SBT_VERSION/sbt-launch.jar

WORKDIR /home/working
COPY target/universal/life-1.0-SNAPSHOT.zip ./
COPY target/universal/life-1.0-SNAPSHOT.zip ./
RUN start ./start