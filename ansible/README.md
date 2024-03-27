# Ansible Notes

# Installing letsencrypt

I'm not too sure how to get this going via ansible, so currently, here are the manual steps

1. Ensure nginx is running via `systemctl status nginx` If it's not, use `systemctl start nginx`
2. Install certbot for nginx `sudo yum install certbot-nginx`
3. Edit `sudo vi /etc/nginx/nginx.conf` and ensure that `server_name` is set to your domain name
4. Test the configuration with `sudo nginx -t`
5. Restart nginx with `sudo systemctl reload nginx`
6. Request a certificate with `sudo certbot --nginx -d my.domain.name`