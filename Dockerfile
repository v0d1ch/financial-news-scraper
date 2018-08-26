FROM heroku/heroku:18

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Recently added 
RUN apt-get install -y --assume-yes libc6 libssl-dev libcurl4-gnutls-dev

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create /opt/financial-news-scraper/bin and /opt/financial-news-scraper/src.  Set
# /opt/financial-news-scraper/src as the working directory.
RUN mkdir -p /opt/financial-news-scraper/src
RUN mkdir -p /opt/financial-news-scraper/bin
WORKDIR /opt/financial-news-scraper/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/financial-news-scraper/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/financial-news-scraper/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./financial-news-scraper.cabal /opt/financial-news-scraper/src/financial-news-scraper.cabal

RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/financial-news-scraper/src
RUN stack -v --no-terminal build

# Install application binaries to /opt/financial-news-scraper/bin.
RUN stack --no-terminal --local-bin-path /opt/financial-news-scraper/bin install

# Remove source code.
#RUN rm -rf /opt/financial-news-scraper/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/financial-news-scraper
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/financial-news-scraper/bin"

# Set the working directory as /opt/financial-news-scraper/.
WORKDIR /opt/financial-news-scraper

CMD /opt/financial-news-scraper/bin/financial-news-scraper

