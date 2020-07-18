COPY @schema.@tableName FROM '@csvFilePath' WITH DELIMITER E'\t' CSV HEADER QUOTE E'\b';
