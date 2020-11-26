FROM gcc:10.2
COPY main_clock.c .
COPY main_gettimeofday.c .
COPY main_times.c .
RUN gcc main_clock.c -o main_clock
RUN gcc main_gettimeofday.c -o main_gettimeofday
RUN gcc main_times.c -o main_times
