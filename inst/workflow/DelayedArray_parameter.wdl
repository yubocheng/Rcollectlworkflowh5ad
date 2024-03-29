version 1.0

task Rcollectl_h5ad_DelayedArray_parameters {
    input {
        Boolean knitr_eval
        String file_url
        String sample
        Int mem_gb = 120
    }

    command {
        /tmp/run_vignette.R ${knitr_eval} ${file_url} ${sample} ${mem_gb}
    }

    output {
        File DelayedArray_parameters_html = "workflow_cellxgene_h5ad_DelayedArray_parameters.html"
        Array[File] Rcollectl_result = glob("*.tab.gz")
    }

    runtime {
        docker: "ycheng2022/bioconductor_docker_workflow_h5ad_delayedarray_parameters:devel"
        memory: mem_gb + "GB"
        cpu: 3
    }
}

workflow RcollectlWorkflowDelayedArrayParameters {
    meta {
        description: "Provide workflow running performance of H5AD file readed in as a SingleCellExperiment object with assays loaded as DelayedArray with different parameters"
    }
    
    input {
        Boolean knitr_eval
        String file_url
        String sample
        Int mem_gb = 120
    }

    call Rcollectl_h5ad_DelayedArray_parameters {
        input: 
        knitr_eval = knitr_eval, 
        file_url = file_url, 
        sample = sample,
        mem_gb = mem_gb
    }
    
    output {
    	File DelayedArray_parameters_html = Rcollectl_h5ad_DelayedArray_parameters.DelayedArray_parameters_html
    	Array[File] Rcollectl_result = Rcollectl_h5ad_DelayedArray_parameters.Rcollectl_result
    }
}
