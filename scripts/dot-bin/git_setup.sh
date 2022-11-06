#!/usr/bin/env sh

set -e

pkg_add git stagit

user add -m git
cd /home/git
su git <<ENDGIT

mkdir -p .ssh && chmod 700 .ssh
touch .ssh/authorized_keys && chmod 600 .ssh/authorized_keys

cat > ~/.ssh/authorized_keys << "END"
no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILcXnswpV545z89oFFWY0YuoAIFDShZ7+OdfFTWYCbtL me@konyahin.xyz
END
ENDGIT

chsh -s /usr/local/bin/git-shell git

cat >> /etc/httpd.conf << "END"

server "git.konyahin.xyz" {
  listen on * tls port 443
  root "/htdocs/git"
  tls {
    certificate "/etc/ssl/konyahin.xyz.fullchain.pem"
    key "/etc/ssl/private/konyahin.xyz.key"
  }

  location "./well-known/acme-challenge/*" {
    root "/acme"
    request strip 2
  }

  location "*/style.css" {
    request rewrite "/style.css"
  }

  location "*/logo.png" {
    request rewrite "/logo.png"
  }

  location "*/favicon.png" {
    request rewrite "/favicon.png"
  }
}
END

mkdir -p /var/www/htdocs/git
chown -R git:git /var/www/htdocs/git

httpd -n
rcctl restart httpd

mkdir -p /git/
chown -R git:git /git

rcctl enable gitdaemon
rcctl set gitdaemon flags --export-all --base-path="/git"
rcctl set gitdaemon user git
rcctl start gitdaemon

cat > /usr/local/share/git-core/templates/hooks/post-receive << "END"
#!/usr/bin/env sh

set -e

rm -rf /var/www/htdocs/git/index.html

cd /git/
for repo in */ ; do
    mkdir -p /var/www/htdocs/git/$repo
    cd /var/www/htdocs/git/$repo
    stagit /git/$repo
done

cd /var/www/htdocs/git/
stagit-index /git/* >> index.html
END

chmod +x /usr/local/share/git-core/templates/hooks/post-receive
