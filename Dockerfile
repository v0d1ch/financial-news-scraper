FROM vodich/investments-info:latest

RUN id -u apiuser &>/dev/null || useradd -ms /bin/bash apiuser
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/finacial-new-scraper/bin"
COPY . /opt/financial-news-scraper/
RUN rm -rf /opt/financial-news-scraper/.stack-work
RUN stack build  --allow-different-user --local-bin-path /opt/financial-news-scraper/bin
CMD /opt/financial-news-scraper/bin/financial-news-scraper
