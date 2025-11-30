import pytest
from typing import cast

from scoot_core.dialect.mssql import MSSQLTypeMapper
from scoot_core.types import String, Collation, Integer, TypeAdapter

from sqlglot import exp, parse_one
from sqlalchemy.dialects.mssql import VARCHAR, INTEGER, BIT

mapper = MSSQLTypeMapper()


@pytest.mark.parametrize(
    "case",
    [
        {"name": "SqlAlchemyAdapter", "input": TypeAdapter.get_instance(BIT())},
        {
            "name": "SqlGlotAdapter",
            "input": TypeAdapter.get_instance(
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
    scoot_type, sqla_type, native_type = mapper.resolve_type(
        TypeAdapter.get_instance(INTEGER())
    )

    # Then
    assert isinstance(scoot_type, Integer)
    assert scoot_type.bits == 64
    assert scoot_type.signed

    assert sqla_type == "INTEGER"

    assert native_type == "int"


def test_varchar_conversion():

    # When
    scoot_type, sqla_type, native_type = mapper.resolve_type(
        TypeAdapter.get_instance(VARCHAR(50, "SQL_Latin1_General_CP1_CI_AS"))
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
