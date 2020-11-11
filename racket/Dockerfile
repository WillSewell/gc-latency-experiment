FROM jackfirth/racket:6.10
COPY main.rkt .
COPY docker-entrypoint.sh .
RUN chmod +x docker-entrypoint.sh
RUN raco pkg install --deps search-auto --batch gcstats
ENTRYPOINT ["./docker-entrypoint.sh", "racket", "-l", "gcstats", "-t", "main.rkt"]
