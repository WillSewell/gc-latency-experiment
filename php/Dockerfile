FROM php:7.1-cli
COPY main.php .
ENTRYPOINT ["php", "--define", "memory_limit=256M", "main.php"]
