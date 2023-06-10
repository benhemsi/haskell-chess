FROM ubuntu:20.04
RUN mkdir -p /chess/chess-app
ARG BINARY_PATH
WORKDIR /chess
RUN apt-get update && apt-get install -y libpq-dev
COPY .stack-work/install/x86_64-linux-tinfo6/3e8262f63fe32efb8f698f489b0cca67f52ac710d0084caa03b0f552ad65a94a/8.10.7/bin/chess-exe /chess/chess-app
CMD ["/chess/chess-app/chess-exe"]
