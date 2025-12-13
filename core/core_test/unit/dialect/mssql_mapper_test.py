from typing import cast

import pytest
from scoot_core.dialect.mssql import MSSQLTypeMapper
from scoot_core.types import Collation, Integer, String, type_adapter
from sqlalchemy.dialects.mssql import BIT, INTEGER, VARCHAR
from sqlglot import exp, parse_one

mapper = MSSQLTypeMapper()


@pytest.mark.parametrize(
    "case",
    [
        {"name": "SqlAlchemyAdapter", "input": type_adapter(BIT())},
        {
            "name": "SqlGlotAdapter",
            "input": type_adapter(
                cast(
                    exp.ColumnDef,
                    parse_one("CREATE TABLE test (name BIT)", dialect="tsql").find(
                        exp.ColumnDef
                    ),
                )
            ),
        },
    ],
    ids=lambda c: c["name"],
)
def test_bit_conversion(case):
    # When
    scoot_type, sqla_type, native_type = mapper.resolve_type(case["input"])

    # Then
    assert isinstance(scoot_type, Integer)
    assert scoot_type.bits == 1
    assert not scoot_type.signed

    assert sqla_type == "BIT"

    assert native_type == "bit"


def test_integer_conversion():
    # When
    scoot_type, sqla_type, native_type = mapper.resolve_type(type_adapter(INTEGER()))

    # Then
    assert isinstance(scoot_type, Integer)
    assert scoot_type.bits == 64
    assert scoot_type.signed

    assert sqla_type == "INTEGER"

    assert native_type == "int"


def test_varchar_conversion():
    # When
    scoot_type, sqla_type, native_type = mapper.resolve_type(
        type_adapter(VARCHAR(50, "SQL_Latin1_General_CP1_CI_AS"))
    )

    # Then
    assert isinstance(scoot_type, String)
    assert scoot_type.max_len == 50
    assert scoot_type.collation
    assert (
        scoot_type.collation.to_dict()
        == Collation(
            locale="Latin1_General_CP1", case_sensitive=False, accent_sensitive=True
        ).to_dict()
    )
    assert not scoot_type.lob

    assert sqla_type == 'VARCHAR(50) COLLATE "SQL_Latin1_General_CP1_CI_AS"'
    assert native_type == "varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS"
