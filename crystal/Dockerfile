FROM crystallang/crystal:1.3.0
COPY main.cr .
RUN crystal build main.cr
ENTRYPOINT ["./main"]
