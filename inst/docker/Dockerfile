# Docker inheritance
FROM bioconductor/bioconductor_docker:devel
ENV knitr_eval FALSE
ENV fileId "e9e0243b-ed2b-4b6d-b049-7c7be43dd5b9"
ENV sample "Disease.group"
ENV core 10
ENV mem_gb 60

# download and install collectl
RUN wget "https://sourceforge.net/projects/collectl/files/latest/download" \
	&& tar -xvzf download \
	&& cd collectl-* \
	&& ./INSTALL \
	&& cd .. \
	&& rm -rf download \
	&& rm -rf collectl-* 

# install Rcollectl with the vignette
COPY install_Rcollectl.R /tmp/install_Rcollectl.R
COPY run_Rcollectl.R /tmp/run_Rcollectl.R

RUN chmod +x /tmp/install_Rcollectl.R
RUN chmod +x /tmp/run_Rcollectl.R

RUN /tmp/install_Rcollectl.R

WORKDIR /output

CMD /tmp/run_Rcollectl.R $knitr_eval $fileId $sample $core $mem_gb
