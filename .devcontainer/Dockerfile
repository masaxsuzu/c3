FROM rust:1.42
ENV DEBIAN_FRONTEND=noninteractive
RUN apt update && apt install -y gcc make binutils libc6-dev gdb
RUN git clone https://github.com/masaxsuzu/dotfiles.git ~/dotfiles && bash ~/dotfiles/deploy.sh