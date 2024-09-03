# Makefile for managing Docker-based RStudio setup
#
# Targets:
#   go:                      Alias for 'start' target.  Also the default target.
#   help:                    Display this help message.
#   setup:                   Install docker infrastructure and ensure the system is up to date.
#   generate-dockerfile:     Generate the Dockerfile if it doesn't already exist.
#   generate-rprofile:       Generate the .Rprofile if it doesn't already exist.
#   build:                   Build the Docker image.
#   create:                  Create the Docker container.
#   start:                   Start the Docker container. It builds the image if necessary, and restarts if stopped.
#   stop:                    Stop the Docker container if it is running.
#   clean-container:         Remove the Docker container.
#   clean-image:             Remove the Docker image.
#   clean-all:               Clean both the Docker container and image.
#   status:                  Output whether the image is built, the container is created, and the container is running.
#   check-setup:             Check if Docker infrastructure is installed.
#   check-image-built:       Check if the Docker image is built.
#   check-container-created: Check if the Docker container is created.
#   check-container-running: Check if the Docker container is running.
#
# Example Usage:
#   make
#   make stop
#   make status
#   make clean-all

# Variables:
CONTAINER_NAME = rstudio-stan
USERNAME = vagrant
PASSWORD = vagrant
HOST_PORT = 8789

# Define aliases
.PHONY: go
go: start

# Get host information
host = $(uname -a)

# Define Dockerfile content
define DOCKERFILE_CONTENT

# Use the rocker/rstudio base image
FROM rocker/rstudio:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies for R and Stan
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libglu1-mesa-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libboost-all-dev \
    cmake \
    gfortran \
    make \
    build-essential \
    g++ \
    f2c \
    libblas-dev \
    liblapack-dev \
    libpcre3-dev \
    libreadline-dev \
    libgsl-dev \
    autoconf \
    libtool* && apt-get clean

# Install rstan and other necessary R packages
RUN R -e "install.packages(c('Rmisc','parallel','coda'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rstan', repos='https://cloud.r-project.org/')"

# Expose the port for RStudio Server
EXPOSE 8787

# Add a user for RStudio
ARG USERNAME
ARG PASSWORD
RUN useradd -m -d /home/$${USERNAME} -G sudo -s /bin/bash $${USERNAME} \
    && echo "$${USERNAME}:$${PASSWORD}" | chpasswd

# Add the project directory
RUN mkdir -p /home/$${USERNAME}/project

# Set permissions for /home/$${USERNAME}
RUN chown -R $${USERNAME}:$${USERNAME} /home/$${USERNAME}

# Copy .Rprofile to root
COPY .Rprofile /home/$${USERNAME}/.Rprofile

# Set the default command to launch RStudio Server
CMD ["/init"]

# Add the user to the sudoers file
RUN echo '$${USERNAME} ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
endef
export DOCKERFILE_CONTENT

# Define .Rprofile content
define RPROFILE_CONTENT
setwd('/home/${USERNAME}/project/')
cat(readLines('README.md'), sep = "\n")
endef
export RPROFILE_CONTENT

# Help
.PHONY: help
help:
	@head -n 32 Makefile

# Generate Dockerfile
.PHONY: generate-dockerfile
generate-dockerfile:
	@if [ -f Dockerfile ]; then \
		echo "Dockerfile already exists."; \
	else \
		echo "$$DOCKERFILE_CONTENT" > Dockerfile; \
		echo "Dockerfile generated."; \
	fi

# Generate .Rprofile
.PHONY: generate-rprofile
generate-rprofile:
	@if [ -f .Rprofile ]; then \
		echo ".Rprofile already exists."; \
	else \
		echo "$$RPROFILE_CONTENT" > .Rprofile; \
		echo ".Rprofile generated."; \
	fi

# Install docker infrastructure
.PHONY: setup
setup:
	@if command -v docker > /dev/null 2>&1; then \
		echo "Docker infrastructure is already installed."; \
	else \
		echo -n "First we need to make sure your system is up to date." && \
			sleep 1 && echo -n "." && sleep 1 && echo "." && sleep 1; \
		sudo apt -y update; \
		sudo apt -y upgrade; \
		echo -n "Now we install Docker." && \
			sleep 1 && echo -n "." && sleep 1 && echo "." && sleep 1; \
		sudo apt -y install docker-buildx && \
			echo "Done with setup."; \
	fi

# Build docker image
.PHONY: build
build: setup generate-dockerfile generate-rprofile
	@if sudo docker image inspect $(CONTAINER_NAME) > /dev/null 2>&1; then \
		echo "Docker image $(CONTAINER_NAME) already built."; \
	else \
		echo -n "Building the Docker image. This will take a while."; \
			sleep 1 && echo -n "." && sleep 1 && echo "." && sleep 1; \
		sudo docker build --build-arg USERNAME=$(USERNAME) \
				  --build-arg PASSWORD=$(PASSWORD) \
				  -t $(CONTAINER_NAME) -f ./Dockerfile . && \
		echo "Docker image built."; \
	fi

# Create the docker container
.PHONY: create
create:
	@if sudo docker ps -aq -f name=$(CONTAINER_NAME) | grep -q .; then \
		echo "Docker container $(CONTAINER_NAME) already created."; \
	else \
		echo "Creating Docker container $(CONTAINER_NAME)..."; \
		sudo docker run -d \
			-p $(HOST_PORT):8787 \
			-v $(shell pwd)/project/:/home/$(USERNAME)/project \
			--name $(CONTAINER_NAME) $(CONTAINER_NAME) && \
		echo "Docker container created." && \
		echo "Access RStudio by navigating to http://host:$(HOST_PORT)/ in your web browser." && \
		echo "The username:password is $(USERNAME):$(PASSWORD)"; \
	fi

# Create the docker container
.PHONY: start
start: setup build create
	@if sudo docker ps -q -f name=$(CONTAINER_NAME) | grep -q .; then \
		echo "Docker container $(CONTAINER_NAME) already running on port $(HOST_PORT)."; \
	else \
		sudo docker start $(CONTAINER_NAME) > /dev/null 2>&1 && \
			echo "Docker container $(CONTAINER_NAME) started."; \
	fi
	@echo "Access RStudio by navigating to http://host:$(HOST_PORT)/ in your web browser."
	@echo "The username:password is $(USERNAME):$(PASSWORD)"

# Halt the docker container
.PHONY: stop
stop:
	@if sudo docker ps -q -f name=$(CONTAINER_NAME) | grep -q .; then \
		echo "Stopping Docker container $(CONTAINER_NAME)..." && \
		sudo docker stop $(CONTAINER_NAME) > /dev/null 2>&1 && \
		echo "Docker container $(CONTAINER_NAME) stopped."; \
	else \
		echo "Docker container $(CONTAINER_NAME) not running."; \
	fi


# Cleanup Dockerfile
.PHONY: clean-Dockerfile
clean-Dockerfile:
	@rm Dockerfile && \
		echo "Dockerfile removed."

# Cleanup .Rprofile
.PHONY: clean-Rprofile
clean-Rprofile:
	@rm .Rprofile && \
		echo ".Rprofile removed."

# Cleanup docker container
.PHONY: clean-container
clean-container:
	@sudo docker rm -f $(CONTAINER_NAME) && \
		echo "Docker container removed."

# Cleanup docker image
.PHONY: clean-image
clean-image:
	@sudo docker rmi -f $(CONTAINER_NAME) && \
		echo "Docker image removed."

# Cleanup both docker container and image
.PHONY: clean-all
clean-all: clean-container clean-image clean-Rprofile clean-Dockerfile


# Status of Docker environment
.PHONY: status
status: check-setup check-image-built check-container-created check-container-running

# Check if the Docker infrastructure is installed
.PHONY: check-setup
check-setup:
	@if command -v docker > /dev/null 2>&1; then \
		echo "Docker infrastructure is installed."; \
	else \
		echo "Docker infrastructure is not installed. Please install docker-buildx."; \
	fi

# Check if the Docker image is built
.PHONY: check-image-built
check-image-built:
	@if sudo docker image inspect $(CONTAINER_NAME) > /dev/null 2>&1; then \
		echo "Docker image $(CONTAINER_NAME) is built."; \
	else \
		echo "Docker image $(CONTAINER_NAME) is not built."; \
	fi

# Check if the Docker container is created
.PHONY: check-container-created
check-container-created:
	@if sudo docker ps -aq -f name=$(CONTAINER_NAME) | grep -q .; then \
		echo "Docker container $(CONTAINER_NAME) is created."; \
	else \
		echo "Docker container $(CONTAINER_NAME) is not created."; \
	fi

# Check if the Docker container is running
.PHONY: check-container-running
check-container-running:
	@if sudo docker ps -q -f name=$(CONTAINER_NAME) | grep -q .; then \
		echo "Docker container $(CONTAINER_NAME) is running on port $(HOST_PORT)."; \
	else \
		echo "Docker container $(CONTAINER_NAME) is not running."; \
	fi

