FROM crystallang/crystal:1.4
COPY main.cr .
RUN crystal build main.cr
ENTRYPOINT ["./main"]
