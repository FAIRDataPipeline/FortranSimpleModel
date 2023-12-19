# FortranSimpleModel

Simple SEIRS model using the Fortran API for the FAIR data pipeline. This depends on
the C/C++ API.

## Installation

From the top-level directory:

```bash
$ cmake -Bbuild .
$ cmake --build build
```

## Run locally

From the top level of the project:

```bash
$ ./build/bin/fortran_simple_model data/local_data.csv
```

The produced data will be located in the directory `data_store`.

## Run using FAIR Data Pipeline


First, install the [`fair` CLI](https://github.com/FAIRDataPipeline/FAIR-CLI).

```bash
$ pip install fair-cli
```

To set up the run, first start a local registry:

```bash
$ fair registry install
$ fair registry start
```

Note the location of the registry token reported here. If using default settings, this
should be at `/home/USERNAME/.fair/registry/token`. Then, initialise the repository:

```bash
$ fair init [--local]
```

Finally, run the model using:

```bash
$ fair pull data/config.yaml
$ fair run data/config.yaml
```

If you don't have access to the remote server, a local run can be performed using:

```bash
$ fair pull --local data/config.yaml
$ fair run --local data/config.yaml
```

You can also run in a dirty git repo using:

```bash
$ fair run --dirty data/config.yaml
$ # Without remote server access:
$ fair run --local --dirty data/config.yaml
```
