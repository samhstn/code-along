# How to Use Virtual Environments with the Built-In venv Module

(From: https://www.youtube.com/watch?v=Kg1Yvry_Ydk)

To install dependencies into a virtual environment, and to keep packages separate, we use `venv`.

This comes built in to the `python3` standard library.

To see all system wide dependencies, run:

```bash
$ pip3 list
Package    Version
---------- -------
pip        20.0.2
setuptools 46.0.0
wheel      0.34.2
```

To create a new virtual environment, run:
(this is normally created inside the project directory).

```bash
python3 -m venv venv
```

This creates a new directory called `venv`.

We can activate this environment with:

```bash
source venv/bin/activate
```

We can check which environment we have activated with:

```bash
which python
```

Now `pip list` only shows dependencies installed in this virtual environment.

```bash
Package    Version
---------- -------
pip        19.2.3
setuptools 41.2.0
```

We can how install some new dependencies, we'll install `requests` and `pytz`.

Now `pip list` shows:

```bash
Package    Version
---------- ---------
certifi    2020.6.20
chardet    3.0.4
idna       2.10
pip        20.1.1
pytz       2020.1
requests   2.24.0
setuptools 41.2.0
urllib3    1.25.10
```

We can save this list of dependencies with:

```bash
pip freeze > requriements.txt
```

This can be installed by someone else with:

```bash
pip install -r requirements.txt
```

To exit from a virtual env, simply run:

```bash
deactivate
```
