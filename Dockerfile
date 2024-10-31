FROM clfoundation/sbcl:latest

# Install locales
RUN apt update && apt -y install locales wget
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Install PostgreSQL client (update this section)
RUN apt-get update && apt-get install -y wget lsb-release
RUN sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list' && \
    wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    apt-get update && \
    apt-get install -y postgresql-client-14

# Set up PostgreSQL environment variables
ENV PGSSLMODE=require
ENV PGHOST=ep-falling-sunset-a4546ynz-pooler.us-east-1.aws.neon.tech
ENV PGPORT=5432
ENV PGDATABASE=jmdict
ENV PGUSER=default
ENV PGPASSWORD="endpoint=ep-falling-sunset-a4546ynz;mfRe13gPNzSs"

# Install Quicklisp
WORKDIR /root
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --non-interactive \
         --eval "(quicklisp-quickstart:install)" \
         --eval "(ql-util:without-prompting (ql:add-to-init-file))" \
         --eval "(sb-ext:quit)"

# Clone necessary repositories
WORKDIR /root
RUN git clone https://gitlab.com/yamagoya/jmdictdb.git
RUN git clone https://github.com/tshatrov/ichiran.git /root/quicklisp/local-projects/ichiran/

# Copy custom settings and scripts
COPY docker/settings.lisp /root/quicklisp/local-projects/ichiran/settings.lisp
COPY docker/ichiran-scripts /root/quicklisp/local-projects/ichiran/docker/ichiran-scripts

# Build ichiran
WORKDIR /root/quicklisp/local-projects/ichiran
RUN sbcl --non-interactive --eval "(ql:quickload :ichiran)"

# Add ichiran scripts to PATH
ENV PATH="/root/quicklisp/local-projects/ichiran/docker/ichiran-scripts:${PATH}"

# Set entrypoint
ENTRYPOINT ["entrypoint.sh"]
