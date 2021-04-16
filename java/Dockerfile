FROM openjdk:16
COPY Main.java .
RUN javac Main.java
ENTRYPOINT ["java", "-Xmx512m", "-XX:+PrintFlagsFinal", "-verbosegc", "-XX:+UseZGC", "-XX:MaxGCPauseMillis=10", "-XX:ParallelGCThreads=2", "Main"]
