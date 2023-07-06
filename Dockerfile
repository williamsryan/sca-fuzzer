FROM ubuntu:20.04

ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get update -y && apt-get install -y \
        build-essential \
        # linux-headers-$(uname -r) \
        python3.9 python3.9-venv python3.9-distutils python3-pip \
        unicorn \
        git \
        vim \
    && apt-get clean

RUN pip install virtualenv unicorn pyyaml types-pyyaml numpy iced_x86
# RUN virtualenv ~/venv-revizor
# RUN source ~/venv-revizor/bin/activate

WORKDIR /src/x86/isa_spec
RUN ./get_spec.py --extensions BASE SSE SSE2 CLFLUSHOPT CLFSH

COPY ./src src/
WORKDIR /src/x86/executor
RUN make uninstall && make clean && make && make install

WORKDIR /src
ENTRYPOINT ./cli.py fuzz -s x86/isa_spec/base.json -i 50 -n 100 -c tests/test-nondetection.yaml -w . --empty-synth
