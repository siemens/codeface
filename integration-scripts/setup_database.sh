#!/bin/sh

echo "Providing codeface database"

sudo mysql -e "CREATE DATABASE codeface;" -uroot
sudo mysql -e "CREATE USER 'codeface'@'localhost' IDENTIFIED BY 'codeface';" -uroot
sudo mysql -e "GRANT ALL PRIVILEGES ON * . * TO 'codeface'@'localhost';" -uroot

mysql -ucodeface -pcodeface < datamodel/codeface_schema.sql

