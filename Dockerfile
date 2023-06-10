FROM ubuntu:20.04
RUN mkdir -p /chess/chess-app
ARG BINARY_PATH
WORKDIR /chess
RUN apt-get update && apt-get install -y libpq-dev
COPY "$BINARY_PATH" /chess/chess-app
CMD ["/chess/chess-app/chess-exe"]
