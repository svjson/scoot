from setuptools import setup, find_packages

setup(
    name='scoot-server',
    version='0.1.0',
    packages=find_packages(),
    install_requires=[
        'Flask>=2.2.0',
        'orjson>=3.9.0',
        'scoot-core'
    ],
    entry_points={
        'console_scripts': [
            'scoot-server = scoot_server.main:main'
        ]
    },
    python_requires='>=3.8'
)
