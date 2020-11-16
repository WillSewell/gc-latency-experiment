FROM erlang:23.1
COPY main.erl .
RUN erlc -smp *.erl
ENTRYPOINT ["erl", "-noshell", "main", "start", "-s", "init", "stop"]
