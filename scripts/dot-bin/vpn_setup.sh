#!/usr/bin/env sh

set -e 

sysctl net.inet.ip.forwarding=1
sysctl net.inet.ipcomp.enable=1
sysctl net.inet.esp.enable=1
sysctl net.inet.esp.udpencap=1

cat >> /etc/sysctl.conf << "END"

net.inet.ip.forwarding=1
net.inet.ipcomp.enable=1
net.inet.esp.enable=1
net.inet.esp.udpencap=1
END

cat > /etc/hostname.enc0 << "END"
inet 10.0.1.1 255.255.255.0 10.0.1.255
up
END

sh /etc/netstart

echo "user name:"
read NAME
echo "user password:"
read PASSWORD

cat > /etc/iked.conf << "END"
server_ip = "185.92.220.211"
dns1 = "1.1.1.1"
dns2 = "8.8.8.8"

user "$NAME" "$PASSWORD"

ikev2 "responder_eap" passive esp \
    from 0.0.0.0/0 to 10.0.1.0/24 \
    from 10.0.0.0/24 to 10.0.1.0/24 \
    local egress peer any \
    ikesa enc aes-256 prf hmac-sha2-256 auth hmac-sha2-256 group modp2048 \
	childsa enc aes-256 auth hmac-sha2-256 group modp2048 \
    srcid konyahin.xyz \
    eap "mschap-v2" \
    config protected-subnet 0.0.0.0/0 \
    config address 10.0.1.0/24 \
    config name-server $dns1 \
    config name-server $dns2 \
    tag "ROADW"
END

cat >> /etc/pf.conf << "END"

wan = vio0
vpn = enc0
match out on $wan inet nat-to ($wan:0)
pass in quick on $wan inet proto udp from any to ($wan:0) port {500, 4500} keep state label ipsec
pass in quick on $vpn inet keep state (if-bound)
END

pfctl -f /etc/pf.conf

cp /etc/ssl/private/konyahin.xyz.key /etc/iked/private/local.key

csplit -s -fcerts /etc/ssl/konyahin.xyz.fullchain.pem /BEGIN/ {0}
mv certs00 /etc/iked/certs/konyahin.xyz.crt
mv certs01 /etc/iked/ca/ca.crt

echo 'iked_flags=""' >> /etc/rc.conf.local
rcctl enable iked
rcctl start iked
