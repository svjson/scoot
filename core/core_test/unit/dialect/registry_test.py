import sqlalchemy.sql.sqltypes as alchemy
from scoot_core.dialect import resolve_type
import scoot_core.types as scoot


def test_resolve_type__boolean__mariadb():
    # Given
    col_type = alchemy.Boolean()

    # When
    scoot_type, driver_type, native_type = resolve_type("mariadb", col_type)

    # Then
    assert scoot_type is None
    assert driver_type == 'BOOLEAN'
    assert native_type == 'BOOL'


def test_resolve_type__boolean__mssql():
    # Given
    col_type = alchemy.Boolean()

    # When
    scoot_type, driver_type, native_type = resolve_type("mssql", col_type)

    # Then
    assert scoot_type is None
    assert driver_type == 'BOOLEAN'
    assert native_type == 'bit'


def test_resolve_type__boolean__mysql():
    # Given
    col_type = alchemy.Boolean()

    # When
    scoot_type, driver_type, native_type = resolve_type("mysql", col_type)

    # Then
    assert scoot_type is None
    assert driver_type == 'BOOLEAN'
    assert native_type == 'BOOL'


def test_resolve_type__boolean__oracle():
    # Given
    col_type = alchemy.Boolean()

    # When
    scoot_type, driver_type, native_type = resolve_type("oracle", col_type)

    # Then
    assert scoot_type is None
    assert driver_type == 'BOOLEAN'
    assert native_type == 'SMALLINT'


def test_resolve_type__boolean__postgres():
    # Given
    col_type = alchemy.Boolean()

    # When
    scoot_type, driver_type, native_type = resolve_type("postgresql", col_type)

    # Then
    assert scoot_type is not None
    assert (
        scoot_type.to_dict()
        == scoot.Boolean(
            true_literals=["true", "yes", "on", "1"],
            false_literals=["false", "no", "off", "0"],
        ).to_dict()
    )
    assert driver_type == 'BOOLEAN'
    assert native_type == 'BOOLEAN'
