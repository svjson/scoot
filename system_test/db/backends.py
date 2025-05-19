from typing import Union

KvProp = dict[str, str]
Prop = Union[str, int, KvProp]
BackendConfig = dict[str, Prop]


BACKENDS: dict[str, BackendConfig] = {
    "mariadb": {
        "name": "mariadb",
        "image": "mariadb:10.11",
        "port": 13307,
        "target_port": 3306,
        "connection_url": "mysql+pymysql://root:password@localhost:13307/mysql",
        "active_connection_url": "mysql+pymysql://root:password@localhost:13307/nexartrade_staging",
        "env": {"MARIADB_ROOT_PASSWORD": "password"},
        "ping_query": "SELECT 1;",
    },
    "mssql": {
        "name": "mssql",
        "image": "mcr.microsoft.com/mssql/server:2022-CU5-ubuntu-20.04",
        "port": 11433,
        "target_port": 1433,
        "connection_url": "mssql+pyodbc://sa:Password123@localhost:11433/tempdb?driver=ODBC+Driver+18+for+SQL+Server&TrustServerCertificate=yes",
        "active_connection_url": "mssql+pyodbc://nexar:StrongPassword123@localhost:11433/NexarTrade_v1?driver=ODBC+Driver+18+for+SQL+Server&TrustServerCertificate=yes",
        "env": {"ACCEPT_EULA": "Y", "MSSQL_SA_PASSWORD": "Password123"},
        "ping_query": "SELECT 1;",
    },
    "mysql": {
        "name": "mysql",
        "image": "mysql:8.4",
        "port": 13306,
        "target_port": 3306,
        "connection_url": "mysql+pymysql://root:password@localhost:13306",
        "active_connection_url": "mysql+pymysql://root:password@localhost:13306/nexartrade_staging",
        "env": {"MYSQL_ROOT_PASSWORD": "password"},
        "ping_query": "SELECT 1;",
    },
    "oracle_11g": {
        "name": "oracle_11g",
        "image": "wnameless/oracle-xe-11g-r2",
        "port": 11521,
        "target_port": 1521,
        "connection_url": "oracle+cx_oracle://system:oracle@localhost:11521/?service_name=XE",
        "active_connection_url": "oracle+cx_oracle://nexartrade_staging:password@localhost:11521/?service_name=XE",
        "env": {"ORACLE_ENABLE_XDB": "true", "ORACLE_ALLOW_REMOTE": "true"},
        "ping_query": "SELECT 1 FROM DUAL",
    },
    "oracle_23c": {
        "name": "oracle_23c",
        "image": "gvenzl/oracle-free:23.7-slim-faststart",
        "port": 11522,
        "target_port": 1521,
        "connection_url": "oracle+cx_oracle://nexartrade_staging:password@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=localhost)(PORT=11522))(CONNECT_DATA=(SERVICE_NAME=freepdb1)))",
        "env": {
            "ORACLE_PASSWORD": "oracle",
            "APP_USER": "nexartrade_staging",
            "APP_USER_PASSWORD": "password",
        },
        "ping_query": "SELECT 1 FROM DUAL",
    },
    "postgres": {
        "name": "postgres",
        "image": "postgres:15.3",
        "port": 15432,
        "target_port": 5432,
        "connection_url": "postgresql+psycopg2://postgres:password@localhost:15432/postgres",
        "active_connection_url": "postgresql+psycopg2://postgres:password@localhost:15432/nexartrade_v1?options=-c%20search_path=nexartrade_staging",
        "env": {"POSTGRES_PASSWORD": "password"},
        "ping_query": "SELECT 1;",
    },
}
