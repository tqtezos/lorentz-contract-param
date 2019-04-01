FROM alpine
RUN mkdir -p /opt/morley/
WORKDIR /opt/morley
COPY ./tmp/morley ./morley
ENV PATH "$PATH:/opt/morley"
