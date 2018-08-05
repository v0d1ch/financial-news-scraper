FROM vodich/investments-info:latest

RUN id -u apiuser &>/dev/null || useradd -ms /bin/bash apiuser
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/finacial-new-scraper/bin"
ENV PATH "$PATH:app/stack/bin:app/finacial-new-scraper/bin"
COPY . app/financial-news-scraper/
RUN rm -rf app/financial-news-scraper/.stack-work
RUN stack build  --allow-different-user --local-bin-path app/.local/bin/financial-news-scraper
CMD app/.local/bin/financial-news-scraper
