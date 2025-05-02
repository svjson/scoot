from setuptools import setup, find_packages

setup(
    name='scoot-core',
    version='0.1.0',
    packages=find_packages(),
    install_requires=[
        'SQLAlchemy>=2.0',
    ],
    python_requires='>=3.8'
)
