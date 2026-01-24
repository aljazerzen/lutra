DROP TABLE IF EXISTS movies;
CREATE TABLE movies (
    id int4 NOT NULL,
    title text NOT NULL,
    is_released bool NOT NULL
);
