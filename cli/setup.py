
from setuptools import setup, find_packages

setup(
    name='scoot-cli',
    version='0.1.0',
    packages=find_packages(),
    install_requires=[
        'tabulate',
        'scoot-core'
    ],
    entry_points={
        'console_scripts': [
            'scoot = scoot_cli.main:main'
        ]
    },
    python_requires='>=3.8'
)
