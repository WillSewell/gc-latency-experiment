FROM openjdk:8u141
COPY Main.java .
RUN javac Main.java
ENTRYPOINT ["java", "-Xmx512m", "-XX:+PrintFlagsFinal", "-XX:+PrintGCTimeStamps", "-verbosegc", "-XX:+UseG1GC", "-XX:MaxGCPauseMillis=10", "-XX:ParallelGCThreads=2", "Main"]
