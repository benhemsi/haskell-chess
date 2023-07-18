FROM ubuntu:20.04
RUN mkdir -p /chess/chess-app
RUN mkdir -p /chess/config
ARG OPENING_TABLE_BINARY_PATH
ADD ./config/opening-table-config.yaml /chess/config
WORKDIR /chess
RUN apt-get update && apt-get install -y libpq-dev
COPY "$OPENING_TABLE_BINARY_PATH" /chess/chess-app
CMD ["/chess/chess-app/opening-table-exe"]
