version 1.0

task Rcollectl_cellxgene_h5ad {
    input {
        Boolean knitr_eval
        String fileId
        String sample
        Int core = 10
        Int mem_gb = 60
    }

    command {
        /tmp/run_Rcollectl.R ${knitr_eval} ${fileId} ${sample} ${core} ${mem_gb}
    }

    output {
        File workflow_cellxgene_h5ad_html = "workflow_cellxgene_h5ad.html"
        File h5ad_rds = "sce_h5ad.rds"
        File cell_type_classification_rds = "cell_type_classification.rds"
        Array[File] Rcollectl_result = glob("*.tab.gz")
        Array[File] Rcollectl_timestamp = glob("*.timestamp.txt")
    }

    runtime {
        docker: "ycheng2022/bioconductor_docker_workflow_cellxgene_h5ad:devel"
        memory: mem_gb + "GB"
        cpu: core
    }
}

workflow RcollectlWorkflow {
    meta {
        description: "Provide a single-cell analysis Workflow as well as computing resources usage tracking with package Rcollectl"
    }
    
    input {
        Boolean knitr_eval
        String fileId
        String sample
        Int core = 10
        Int mem_gb = 60
    }

    call Rcollectl_cellxgene_h5ad {
        input: 
        knitr_eval = knitr_eval, 
        fileId = fileId, 
        sample = sample,
        core = core,
        mem_gb = mem_gb
    }
    
    output {
    	File workflow_cellxgene_h5ad_html = Rcollectl_cellxgene_h5ad.workflow_cellxgene_h5ad_html
    	File h5ad_rds = Rcollectl_cellxgene_h5ad.h5ad_rds
    	File cell_type_classification_rds = Rcollectl_cellxgene_h5ad.cell_type_classification_rds
    	Array[File] Rcollectl_result = Rcollectl_cellxgene_h5ad.Rcollectl_result
    	Array[File] Rcollectl_timestamp = Rcollectl_cellxgene_h5ad.Rcollectl_timestamp
    }
}
