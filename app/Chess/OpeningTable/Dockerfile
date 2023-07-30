FROM ubuntu:20.04
RUN mkdir -p /chess/chess-app
RUN mkdir -p /chess/config
ARG OPENING_TABLE_BINARY_PATH
ADD ./config /chess/config
WORKDIR /chess
RUN apt-get update && apt-get install -y libpq-dev
ARG CACHE_DATE=
RUN echo "$CACHE_DATE"
ADD "$OPENING_TABLE_BINARY_PATH" /chess/chess-app
CMD ["/chess/chess-app/opening-table-exe"]
