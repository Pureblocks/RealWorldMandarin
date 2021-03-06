CREATE Database realworldmandarin;

CREATE TABLE tweets (id SERIAL PRIMARY KEY, level VARCHAR NOT NULL, create_date timestamptz  NOT NULL, content VARCHAR NOT NULL);

CREATE TABLE hsk_words (id SERIAL PRIMARY KEY, level VARCHAR NOT NULL, simplified VARCHAR NOT NULL, traditional VARCHAR NOT NULL, pinyin_number VARCHAR NOT NULL, pinyin_tones VARCHAR NOT NULL, definition VARCHAR NOT NULL);

CREATE TABLE characters (id INTEGER PRIMARY KEY, hanzi_character VARCHAR NOT NULL, keyword VARCHAR NOT NULL, primitive BOOLEAN NOT NULL);

CREATE TABLE elements (id SERIAL PRIMARY KEY, character__id INTEGER REFERENCES characters(id) NOT NULL, order_position INTEGER NOT NULL, element__id INTEGER REFERENCES characters(id) NOT NULL);

CREATE TABLE users (id SERIAL PRIMARY KEY, username VARCHAR(50) NOT NULL UNIQUE, email VARCHAR(50) NOT NULL UNIQUE, password_hash VARCHAR NOT NULL, salt VARCHAR NOT NULL);

CREATE TABLE user_characters (id SERIAL PRIMARY KEY, user__id INTEGER NOT NULL REFERENCES users(id), character__id INTEGER REFERENCES characters(id) NOT NULL, story VARCHAR NOT NULL);
