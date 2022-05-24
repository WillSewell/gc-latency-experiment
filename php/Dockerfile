FROM php:8.1-cli
COPY main.php .
ENTRYPOINT ["php", "--define", "memory_limit=256M", "main.php"]
