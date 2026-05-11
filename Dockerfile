FROM tercen/runtime-r44-minimal:4.4.3-2 AS build
RUN installr -d data.table
RUN installr -d forcats
RUN installr -d writexl

FROM tercen/runtime-r44-minimal:4.4.3-2
RUN apk add --no-cache jemalloc
ENV LD_PRELOAD=/usr/lib/libjemalloc.so.2
ENV OPENBLAS_NUM_THREADS=1
COPY --from=build /usr/local/lib/R/library /usr/local/lib/R/library
COPY main.R /operator/main.R
COPY utils.R /operator/utils.R
WORKDIR /operator
ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
