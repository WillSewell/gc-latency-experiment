FROM erlang:25.0
COPY main.erl .
RUN erlc -smp *.erl
ENTRYPOINT ["erl", "-noshell", "-s", "main", "start", "-s", "init", "stop"]
