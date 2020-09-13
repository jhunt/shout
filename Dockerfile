FROM huntprod/cl
WORKDIR /cl
ENV BUILD=/lib/cl
ENV CL_SOURCE_REGISTRY=/cl

RUN apt-get update \
 && apt-get install -y curl make \
 && rm -rf /var/lib/apt/lists/*

COPY Makefile .
RUN make quicklisp libs

COPY . .
RUN make shout \
 && mv shout /shout

ARG BUILD_DATE
ARG VCS_REF
LABEL maintainer="James Hunt <images@huntprod.com>" \
      summary="Run SHOUT! in a container" \
      org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.vcs-url="https://github.com/jhunt/shout.git" \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.schema-version="1.0.0"

ENV SHOUT_IT_OUT_LOUD=yes \
    SHOUT_DATABASE=/db \
    SHOUT_PORT=7100 \
    SHOUT_OPS_CREDS=shouty:abouty \
    SHOUT_ADMIN_CREDS=admin:sadmin

EXPOSE 7100
CMD ["/shout"]
