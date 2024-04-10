FROM clfoundation/sbcl

RUN apt-get update \
 && apt-get install -y xinetd

ENTRYPOINT []
CMD ["/bin/sh", "-c", "/usr/sbin/xinetd -dontfork"]

COPY service.xinetd /etc/xinetd.d/snowpro
WORKDIR /app
COPY prolog.cl  .
COPY run-telnet .
