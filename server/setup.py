from setuptools import setup, find_packages

driver_versions = {
    "pyodbc": "pyodbc>=5.2.0,<6.0",
    "psycopg": "psycopg[binary]>=3.2.0,<4.0",
    "pymysql": "pymysql>=1.1,<2.0",
}

setup(
    name='scoot-server',
    version='0.1.0',
    packages=find_packages(),
    install_requires=['Flask>=2.2.0', 'orjson>=3.9.0', 'scoot-core'],
    extras_require={
        "mssql": [driver_versions["pyodbc"]],
        "postgres": [driver_versions["psycopg"]],
        "mysql": [driver_versions["pymysql"]],
    }
    | {k: [v] for k, v in driver_versions.items()},
    entry_points={'console_scripts': ['scoot-server = scoot_server.main:main']},
    python_requires='>=3.8',
)
