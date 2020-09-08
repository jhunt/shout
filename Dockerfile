FROM huntprod/cl-dev AS build
WORKDIR /cl

RUN apt-get update \
 && apt-get install -y curl

ENV BUILD=/lib/cl
ENV CL_SOURCE_REGISTRY=/cl

COPY Makefile .
run make quicklisp libs

COPY . .
RUN make shout

############################################

FROM ubuntu:18.04

RUN apt-get update \
 && apt-get install -y libssl1.0.0 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=build /cl/shout /shout

ENV SHOUT_IT_OUT_LOUD=yes \
    SHOUT_DATABASE=/db \
    SHOUT_PORT=7100 \
    SHOUT_OPS_CREDS=shouty:abouty \
    SHOUT_ADMIN_CREDS=admin:sadmin

EXPOSE 7100
CMD ["/shout"]
