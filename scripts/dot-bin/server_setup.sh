#!/usr/bin/env sh

set -e

rcctl disable httpd
rcctl stop httpd

cat > /etc/acme-client.conf << "END"
authority letsencrypt {
	api url "https://acme-v02.api.letsencrypt.org/directory"
	account key "/etc/acme/letsencrypt-privkey.pem"
}

authority letsencrypt-staging {
	api url "https://acme-staging-v02.api.letsencrypt.org/directory"
	account key "/etc/acme/letsencrypt-staging-privkey.pem"
}

authority buypass {
	api url "https://api.buypass.com/acme/directory"
	account key "/etc/acme/buypass-privkey.pem"
	contact "mailto:me@konyahin.xyz"
}

authority buypass-test {
	api url "https://api.test4.buypass.no/acme/directory"
	account key "/etc/acme/buypass-test-privkey.pem"
	contact "mailto:me@konyahin.xyz"
}

domain konyahin.xyz {
	alternative names { www.konyahin.xyz git.konyahin.xyz vpn.konyahin.xyz }
	domain key "/etc/ssl/private/konyahin.xyz.key"
	domain full chain certificate "/etc/ssl/konyahin.xyz.fullchain.pem"
	sign with letsencrypt
}
END

mkdir -p -m 700 /etc/acme
mkdir -p -m 700 /etc/ssl/acme/private
mkdir -p -m 755 /var/www/acme

cat > /etc/httpd.conf << "END"
server "konyahin.xyz" {
  listen on * port 80
  root "/htdocs/konyahin.xyz"
  location "/.well-known/acme-challenge/*" {
    root "/acme"
    request strip 2
  }
}

server "www.konyahin.xyz" {
  listen on * port 80
  block return 301 "http://konyahin.xyz$REQUEST_URI"
}
END

httpd -n
rcctl enable httpd
rcctl start httpd
acme-client -v konyahin.xyz

cat > /etc/httpd.conf << "END"
server "konyahin.xyz" {
  listen on * tls port 443
  root "/htdocs/konyahin.xyz"
  tls {
    certificate "/etc/ssl/konyahin.xyz.fullchain.pem"
    key "/etc/ssl/private/konyahin.xyz.key"
  }

  location "./well-known/acme-challenge/*" {
    root "/acme"
    request strip 2
  }
}

server "www.konyahin.xyz" {
  listen on * tls port 443
  tls {
    certificate "/etc/ssl/konyahin.xyz.fullchain.pem"
    key "/etc/ssl/private/konyahin.xyz.key"
  }
  block return 301 "https://konyahin.xyz$REQUEST_URI"
}

server "konyahin.xyz" {
  listen on * port 80
  alias "www.konyahin.xyz"
  block return 301 "https://konyahin.xyz$REQUEST_URI"
}
END

mkdir -p /var/www/htdocs/konyahin.xyz
cat > /var/www/htdocs/konyahin.xyz/index.html << "END"
<html>
    <body>
        Test test
    </body>
</html>
END

httpd -n
rcctl restart httpd

(crontab -l 2>/dev/null; echo "05 3 * * * acme-client example.com && rcctl reload httpd") | crontab -
