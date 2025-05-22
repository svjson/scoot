from scoot_core.dialect.mssql import MSSQLTypeMapper
from scoot_core.types import String, Collation, Integer

from sqlalchemy.dialects.mssql import VARCHAR, INTEGER, BIT

mapper = MSSQLTypeMapper()


def test_bit_conversion():

    # When
    scoot_type, sqla_type, native_type = mapper.resolve_type(BIT())

    # Then
    assert isinstance(scoot_type, Integer)
    assert scoot_type.bits == 1
    assert scoot_type.signed == False

    assert sqla_type == "BIT"

    assert native_type == "bit"


def test_integer_conversion():

    # When
    scoot_type, sqla_type, native_type = mapper.resolve_type(INTEGER())

    # Then
    assert isinstance(scoot_type, Integer)
    assert scoot_type.bits == 64
    assert scoot_type.signed == True

    assert sqla_type == "INTEGER"

    assert native_type == "int"


def test_varchar_conversion():

    # When
    scoot_type, sqla_type, native_type = mapper.resolve_type(
        VARCHAR(50, "SQL_Latin1_General_CP1_CI_AS")
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
    assert scoot_type.lob == False

    assert sqla_type == "VARCHAR(50) COLLATE \"SQL_Latin1_General_CP1_CI_AS\""
    assert native_type == "varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS"
