# Snippets
Small lose and fun Haskell fragments

# Installing Postgres on a Debian system, e.g. MX Linux (System V)

> sudo apt install postgresql

> sudo service postgresql status
15/main (port 5432): online

> sudo -i -u postgres
postgres@ > psql
postgres=# CREATE DATABASE mydb;
CREATE DATABASE
postgres=# CREATE USER myuser WITH PASSWORD 'mypassword';
CREATE ROLE
postgres=# GRANT ALL PRIVILEGES ON DATABASE mydb TO myuser;
GRANT
postgres=# \connect mydb
mydb=# GRANT ALL ON SCHEMA public TO myuser;
GRANT
mydb=# \quit

postgres@ > exit
> psql -h localhost mydb myuser
Password for user myuser: (mypassword)
psql (15.10 (Debian 15.10-0+deb12u1))
SSL connection (protocol: TLSv1.3, cipher: TLS_AES_256_GCM_SHA384, compression: off)
Type "help" for help.

mydb=> \dt
         List of relations
 Schema |   Name   | Type  | Owner  
--------+----------+-------+--------
 public | greeting | table | myuser
(1 row)

mydb=> \d greeting
                                  Table "public.greeting"
 Column  |       Type        | Collation | Nullable |               Default                
---------+-------------------+-----------+----------+--------------------------------------
 id      | bigint            |           | not null | nextval('greeting_id_seq'::regclass)
 message | character varying |           | not null | 
Indexes:
    "greeting_pkey" PRIMARY KEY, btree (id)

mydb=> INSERT INTO greeting (message) VALUES ('Aroun soustei'), ('Hej, hallå');
INSERT 0 2

# Create TLS Certificate using Let's Encrypt.

On router, redirect port 80 (and 443) to the same ports on the local server.
Install certbot on the local server:
> sudo apt install certbot

Create a certificate:
> sudo certbot certonly --standalone -d wv2.hopto.org
Saving debug log to /var/log/letsencrypt/letsencrypt.log
Enter email address (used for urgent renewal and security notices)
 (Enter 'c' to cancel): cbrolin@gmail.com

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Please read the Terms of Service at
https://letsencrypt.org/documents/LE-SA-v1.4-April-3-2024.pdf. You must agree in
order to register with the ACME server. Do you agree?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(Y)es/(N)o: Y

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Would you be willing, once your first certificate is successfully issued, to
share your email address with the Electronic Frontier Foundation, a founding
partner of the Let's Encrypt project and the non-profit organization that
develops Certbot? We'd like to send you email about our work encrypting the web,
EFF news, campaigns, and ways to support digital freedom.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(Y)es/(N)o: Y
Account registered.
Requesting a certificate for wv2.hopto.org

Successfully received certificate.
Certificate is saved at: /etc/letsencrypt/live/wv2.hopto.org/fullchain.pem
Key is saved at:         /etc/letsencrypt/live/wv2.hopto.org/privkey.pem
This certificate expires on 2025-05-07.
These files will be updated when the certificate renews.
Certbot has set up a scheduled task to automatically renew this certificate in the background.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If you like Certbot, please consider supporting our work by:
 * Donating to ISRG / Let's Encrypt:   https://letsencrypt.org/donate
 * Donating to EFF:                    https://eff.org/donate-le
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

The above is not as good as it claims. The scheduled task to automatically renew the certificate
will proably fail if the server is running on port 80. TBD

Check the schedules:
> cat /etc/cron.d/certbot
# /etc/cron.d/certbot: crontab entries for the certbot package
#
# Upstream recommends attempting renewal twice a day
#
# Eventually, this will be an opportunity to validate certificates
# haven't been revoked, etc.  Renewal will only occur if expiration
# is within 30 days.
#
# Important Note!  This cronjob will NOT be executed if you are
# running systemd as your init system.  If you are running systemd,
# the cronjob.timer function takes precedence over this cronjob.  For
# more details, see the systemd.timer manpage, or use systemctl show
# certbot.timer.
SHELL=/bin/sh
PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin

0 */12 * * * root test -x /usr/bin/certbot -a \! -d /run/systemd/system && perl -e 'sleep int(rand(43200))' && certbot -q renew --no-random-sleep-on-renew

Check the certificate(s):
> sudo certbot certificates
Saving debug log to /var/log/letsencrypt/letsencrypt.log

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Found the following certs:
  Certificate Name: wv2.hopto.org
    Serial Number: 30297797063aa321400880199f84007fb73
    Key Type: ECDSA
    Domains: wv2.hopto.org
    Expiry Date: 2025-05-07 07:17:14+00:00 (VALID: 89 days)
    Certificate Path: /etc/letsencrypt/live/wv2.hopto.org/fullchain.pem
    Private Key Path: /etc/letsencrypt/live/wv2.hopto.org/privkey.pem
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Service on init V
Startup order is wrong. Despite an explicit dependency to the
postgresql service (and /etc/rc?.d/ links seem correct), init tries to
start this service before postgresql!

Workaround: Disabling Parallel Booting: Setting CONCURRENCY=none in
/etc/init.d/rc forces the system to start services sequentially,
according to the order specified by the symbolic links in directories
like /etc/rc2.d/. This ensures that postgresql starts before
baseServer, as intended.

