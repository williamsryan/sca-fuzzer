FROM ubuntu:20.04

ENV DEBIAN_FRONTEND="noninteractive"

RUN apt-get update -y && apt-get install -y \
        build-essential \
        # linux-headers-$(uname -r) \
        python3.9 python3.9-venv python3.9-distutils python3-pip \
        unicorn \
        git \
        vim \
        wget \
    && apt-get clean

RUN pip install virtualenv unicorn pyyaml types-pyyaml numpy iced_x86
# RUN virtualenv ~/venv-revizor
# RUN source ~/venv-revizor/bin/activate

COPY ./src src/
WORKDIR /src/x86/isa_spec
RUN python3.9 get_spec.py --extensions BASE SSE SSE2 CLFLUSHOPT CLFSH

WORKDIR /src/x86/executor
RUN make docker_uninstall && make clean && make && make docker_install

WORKDIR /src
ENTRYPOINT python3.9 cli.py fuzz -s x86/isa_spec/base.json -i 50 -n 100 -c tests/test-nondetection.yaml -w . --empty-synth
