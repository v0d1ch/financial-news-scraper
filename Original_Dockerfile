FROM vodich/investments-info:latest

RUN id -u apiuser &>/dev/null || useradd -ms /bin/bash apiuser
USER apiuser
ENV PATH "$PATH:/app/.local/stack/bin:/app/.local/finacial-new-scraper/bin"
COPY . /app/.local/financial-news-scraper/
RUN rm -rf /app/.local/financial-news-scraper/.stack-work; exit 0
RUN stack build  --allow-different-user --local-bin-path /app/.local/bin/financial-news-scraper
EXPOSE 5000
CMD /app/.local/bin/financial-news-scraper
