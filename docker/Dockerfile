FROM ubuntu

RUN apt-get update &&\
    apt-get install -y \
            libssl1.0.0 \
    && \
    rm -rf /var/lib/apt/lists/*

COPY shout /shout

ENV SHOUT_IT_OUT_LOUD=yes \
    SHOUT_DATABASE=/db \
    SHOUT_PORT=7100 \
    SHOUT_OPS_CREDS=shouty:abouty \
    SHOUT_ADMIN_CREDS=admin:sadmin

EXPOSE 7100
CMD ["/shout"]
