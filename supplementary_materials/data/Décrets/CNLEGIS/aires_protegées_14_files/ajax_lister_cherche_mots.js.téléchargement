/**
 * Created by PE2 on 11/01/2018.
 */
$(function() {

    var mois_fr= new Array("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre");

    $(".loading").hide();

    function PgLst_docs(){
        var chemin_source = $('#chemin_source').val();
        var chemin_Lst = chemin_source+"page_data_result_direct_mots";
        var pg_limit=$('#page_limite').val();
        $.ajax(
            {
                type:'POST',
                url:chemin_Lst,
                data:{var_pg_limit: pg_limit},
                dataType: 'json',
                beforeSend: function() {
                    $(".loading").show();
                },
                success:function(data){
                    if (data.aaData!='')
                    {
                        $('#Lst_docs').dataTable().fnClearTable();
                        $('#Lst_docs').dataTable().fnAddData(data.aaData);
                        $('#Lst_docs').dataTable().fnDraw();
                    }
                },
                complete: function(){
                    $('.loading').hide();
                },
                error:function()
                {
                    //alert("Improssibe d'exécuter l'affichage des données !!");
                    /*swal({
                     title: "<span style=\"color: #bc332a\">Improssibe d'exécuter l'affichage des données; ERREUR.<span>",
                     type:"error",
                     html: true
                     });*/
                }
            });

    };

    $('#Lst_docs').DataTable( {
        //"ajax": chemin,
        "language": {
            "lengthMenu": "Nbr par page _MENU_",
            "zeroRecords": "&nbsp",//Aucun enregistrement trouvé
            "info": "page _PAGE_ à _PAGES_",
            "infoEmpty": "0 enregistrement",
            "infoFiltered": "",
            "search": "Rechercher"
        },
        "paging": false,
        "filter": false,
        "info": false,
        //responsive: true,
        responsive: {
            details: {
                display: $.fn.dataTable.Responsive.display.modal( {
                    header: function ( row ) {
                        var data = row.data();
                        //return 'Details for '+data[1]+' '+data[2];
                        return "<span class=\"col-teal\">DETAIL</span>";
                    }
                } ),
                renderer: function ( api, rowIdx, columns ) {
                    var data = $.map( columns, function ( col) {

                        if (col.title!='DOC PDF FR'
                            && col.title!='NUM'
                            && col.title!='TEXTE(S)'
                            && col.title!='DATE JO FR'
                            && col.title!='NUM JO'
                            && col.title!='DATE JO'
                            && col.title!='PAGE JO'
                        ){

                            return '<tr >'+
                                '<td width="35%">' +
                                '<b>'+col.title+':'+'</b>' +
                                '</td> '+
                                '<td>' +
                                ''+col.data.replace(/15px|15pt|14pt|13pt|12pt|11pt|10.0pt|10pt|9pt|8pt|7pt|6pt/gi,'13pt')+'' +
                                '</td>'+
                                '</tr>';

                        }

                    } ).join('');
                    return $('<table class="table dtr-details" width="100%"/>').append( data );
                }
            }
        },
        "aLengthMenu": [[10], [10]],
        //"order": [[ 18, "desc" ]],
        "order":false,
        "autoWidth": true,
        "columns": [
            { "data": null,"name": "dates","class":"text-left","Title":"DATES","orderable": false,
                "render": function ( data, type, row, meta ) {
                    var data_date_txt=data['date_txt'];

                    var explod_date_txt=data_date_txt.split('-');
                    var dt_txt_fr= explod_date_txt[0]+' '+ mois_fr[explod_date_txt[1]-1] +' '+explod_date_txt[2];

                    return dt_txt_fr;
                }
            },
            { "data": "date_txt","name": "date_txt","class":"text-left","Title":"DATES"},
            { "data": null,
                "name": "texte",
                "class":"text-left",
                "orderable": false,
                "Title":"TEXTES",
                "render": function ( data, type, row, meta ) {
                    var data_type=data['type_txt'];
                    var data_num=data['num_txt'];
                    var data_obj=data['objet_txt'].replace(/http:\/\/localhost\/legisapplications\//g,$('#chemin_source').val());
                    var data_etat=data['etat_txt'];

                    var data_num_jo=data['num_jo'];
                    var data_dt_jo=data['date_jo'];
                    var data_pg_jo=data['page_jo'];


                    var data_etat_test='';
                    if (data_etat!='En vigueur'){
                        data_etat_test='<br/><p></p><span class="font-bold col-pink">ETAT : '+data_etat+'</span>';
                    }else{
                        data_etat_test='<br/><p></p><span class="font-bold col-teal">ETAT : '+data_etat+'</span>';//.replace(/font-size: 9pt/g,'font-size: 16px')
                    }

                    var explod_date=data_dt_jo.split('-');
                    var dt_jo_fr= explod_date[0]+' '+ mois_fr[explod_date[1]-1] +' '+explod_date[2];

                    var jorm='';
                    if (data_num_jo=='VOIR_J.O' || data_num_jo=='VOIR_JO'){
                        jorm='';//'<span class="col-blue-grey">N° J.O: </span><span class="col-indigo">'+data_num_jo.replace('VOIR_J.O','VOIR J.O')+ '</span>';
                    }else{
                        jorm='<p></p><span class="col-blue-grey">N° J.O: </span><span class="col-indigo">'+data_num_jo+'</span><span class="col-blue-grey"> Date J.O: </span><span class="col-indigo">'+dt_jo_fr+'</span><span class="col-blue-grey"> Page J.O: </span><span class="col-indigo">'+data_pg_jo+'</span>';
                    }

                    var test_rtf=data['html_fichier_fr'];
                    if (test_rtf.length>70){
                        var data_view_rtf_fr="<br/><p></p>"+
                            "<a id='View_html' type='button' href='javascript:void(0)' class='btn btn-danger waves-effect m-r-10' data-rel='tooltip' title='Fichier_VF.html'>" +
                            "<i class='fa fa-eye'></i> <span>Voir le texte (VF)&nbsp;</span>" +
                            "</a>"
                        ;
                    }else{
                        var data_view_rtf_fr="";
                    }

                    var test_rtmg=data['html_fichier_mg'];
                    if (test_rtmg.length>70){
                        var data_view_rtf_mg=""+
                            "<a id='View_html_mg' type='button' href='javascript:void(0)' class='btn btn-success waves-effect ' data-rel='tooltip' title='Fichier_VM.html'>" +
                            "<i class='fa fa-eye'></i> <span>Voir le texte (VM)</span>" +
                            "</a>"
                        ;
                    }else{
                        var data_view_rtf_mg="";
                    }

                    return '<b>'+data_type+' n° '+data_num+'</b>'+'<br/>'+data_obj.replace(/15px|15pt|14pt|13pt|12pt|11pt|10.0pt|10pt|9pt|8pt|7pt|6pt/gi,'13pt')+''+jorm+''+data_etat_test+''+data_view_rtf_fr+' '+data_view_rtf_mg;

                }},
            { "data": "type_txt","name": "type_txt","class":"text-left","Title":"TYPE" },
            { "data": "num_txt","name": "num_txt","class":"text-left","Title":"NUM TEXTE" },
            { "data": "int_name","name": "int_name","class":"text-left","Title":"NUM" },
            { "data": "objet_txt","name": "objet_txt","class":"text-left","Title":"OBJET" },
            { "data": "objet_txt_mg","name": "objet_txt_mg","class":"text-left","Title":"OBJET MG" },
            { "data": "num_jo","name": "num_jo","class":"text-center","Title":"NUM JO" },
            { "data": null,"name": "date_jo_fr","class":"text-center","Title":"DATE JO",
                "render": function ( data, type, row, meta ) {
                    var data_dt_jo_2=data['date_jo'];
                    var explod_date_jo_2=data_dt_jo_2.split('-');
                    var dt_jo_fr_2= explod_date_jo_2[0]+' '+ mois_fr[explod_date_jo_2[1]-1] +' '+explod_date_jo_2[2];
                    return dt_jo_fr_2;
                }
            },
            { "data": "date_jo","name": "date_jo","class":"text-center","Title":"DATE JO FR"},
            { "data": "page_jo","name": "page_jo","class":"text-center","Title":"PAGE JO" },
            { "data": "etat_txt","name": "etat_txt","class":"text-center","Title":"ETAT" },
            { "data": null,"name": "notes","class":"text-left","Title":"NOTES","orderable": false,
                "render": function ( data, type, row, meta ){
                    var val_notes=data['notes'].replace(/http:\/\/localhost\/legisapplications\//g,$('#chemin_source').val());
                    return val_notes.replace(/15px|15pt|14pt|13pt|12pt|11pt|10.0pt|10pt|9pt|8pt|7pt|6pt/gi,'13pt');
                }
            },
            { "data": "notes_mg","name": "notes_mg","class":"text-left","Title":"NOTES MG","orderable": false },
            { "data": "version_pdf_fr","name": "version_pdf_fr","class":"text-left","Title":"DOC PDF FR" },
            { "data": "version_pdf_mg","name": "version_pdf_mg","class":"text-left","Title":"DOC PDF MG" },
            { "data": "ministere","name": "ministere","class":"text-left","Title":"MINISTERE" },
            { "data": "id",
                "name": "id",
                "class":"text-center",
                "Title":"id"
            },
            { "data": "html_fichier_fr","name": "html_fichier_fr","class":"text-left","Title":"HTML FR","orderable": false },
            { "data": "html_fichier_mg","name": "html_fichier_mg","class":"text-left","Title":"HTML MG","orderable": false }
        ],
        "ajax":PgLst_docs()
    });

    var pdf_Table = $('#Lst_docs').DataTable();

    $('#Lst_docs tbody').on( 'click', '#View_pdf', function () {
        //alert('View');
        $('#View_pdf').unbind();

        var pdf_Index = $(this).parents('tr').index();
        var data_pdf_fr = pdf_Table.column( 'version_pdf_fr:name' ).data();
        var data_pdf_mg = pdf_Table.column( 'version_pdf_mg:name' ).data();

        var hauteur_fenetre = $(window).height();
        var hautfn=hauteur_fenetre-200+'px';

        var chemin_source_pdf_consultation = $('#chemin_source').val();

        if (data_pdf_fr[pdf_Index]=='')
        {
            $('#body_modal_view_pdf_fr').html("<p class='font-bold col-pink'>Aucun fichier pdf</p>");
        }else{
            $('#body_modal_view_pdf_fr').html('<object width="100%" height="'+hautfn+'" align="center" data="'+chemin_source_pdf_consultation+'jqupload_2/uploads/'+data_pdf_fr[pdf_Index]+'" type="text/html" codetype="application/pdf" >' +
                '<param name="filename" value="'+data_pdf_fr[pdf_Index]+'" /> ' +
                '<a class="btn btn-info waves-effect" href="'+chemin_source_pdf_consultation+'jqupload_2/uploads/'+data_pdf_fr[pdf_Index]+'" title="Fichier .pdf" >' +
                '<i class="material-icons">get_app</i>' +
                ' Télécharger le fichier</a>' +
                '</object>');
        }

        if (data_pdf_mg[pdf_Index]==''){
            $('#body_modal_view_pdf_mg').html("<p class='font-bold col-pink'>Aucun fichier pdf</p>");
        }else{
            $('#body_modal_view_pdf_mg').html('<object width="100%" height="'+hautfn+'" align="center" data="'+chemin_source_pdf_consultation+'jqupload_2/uploads/'+data_pdf_mg[pdf_Index]+'" type="text/html" codetype="application/pdf" >' +
                '<param name="filename" value="'+data_pdf_mg[pdf_Index]+'" /> ' +
                '<a class="btn bg-green waves-effect" href="'+chemin_source_pdf_consultation+'jqupload_2/uploads/'+data_pdf_mg[pdf_Index]+'" title="Fichier .pdf" >' +
                '<i class="material-icons">get_app</i>' +
                ' Télécharger le fichier</a>' +
                '</object>');
        }

        $('#confirm-view-pdf').modal('show');

        //alert('View_pdf');
    });

    var html_Table = $('#Lst_docs').DataTable();

    $('#Lst_docs tbody').on( 'click', '#View_html', function () {
        //alert('View');
        $('#View_html').unbind();

        var html_Index = $(this).parents('tr').index();

        var data_id = html_Table.column( 'id:name' ).data();
        var html_fr = html_Table.column( 'html_fichier_fr:name' ).data();
        //var html_mg = html_Table.column( 'html_fichier_mg:name' ).data();

        $('#txt_num_texte').val(data_id[html_Index]);

        $('#body_modal_view_html_fr').html(html_fr[html_Index].replace(/http:\/\/localhost\/legisapplications\//g,$('#chemin_source').val()));
        //$('#body_modal_view_html_mg').html(html_mg[html_Index].replace(/http:\/\/localhost\/legisapplications\//g,$('#chemin_source').val()));

        $('#confirm-view-html').modal('show');
        $('#fermer_modal').show();

        $('#btnfermer_view_html').on('click',function(){
            $('#btnfermer_view_html').unbind();
            $('#View_html').unbind();
        });
        //alert('View_html');
    });

    //21-10-2020-------------------------------------------
    $('#btn_dompdf_content_fr').click(function () {
        var data_id_texte=$('#txt_num_texte').val();

        var id_cript=data_id_texte.replace('0','akZ').replace('1','blY').replace('2','cmX').replace('3','dnW').replace('4','eoV').replace('5','fpU').replace('6','gqT').replace('7','hrS').replace('8','isR').replace('9','jtQ');

        var chemin_source = $('#chemin_source').val();
        //var chemin_pdf = chemin_source+"page_pdf/"+id_cript;
        var chemin_pdf = chemin_source+"page_acces_pdf/"+id_cript;
        window.open(chemin_pdf,'_blank');
    });

    $('#btn_dompdf_content_mg').click(function () {
        var data_id_texte_mg=$('#txt_num_texte_mg').val();

        var id_cript=data_id_texte_mg.replace('0','akZ').replace('1','blY').replace('2','cmX').replace('3','dnW').replace('4','eoV').replace('5','fpU').replace('6','gqT').replace('7','hrS').replace('8','isR').replace('9','jtQ');

        var chemin_source = $('#chemin_source').val();
        var chemin_pdf_mg = chemin_source+"page_pdf_mg/"+id_cript;
        window.open(chemin_pdf_mg,'_blank');

    });

    $('#Lst_docs tbody').on( 'click', '#View_html_mg', function () {
        //alert('View');
        $('#View_html_mg').unbind();

        var html_Index = $(this).parents('tr').index();

        var data_id = html_Table.column( 'id:name' ).data();
        //var html_fr = html_Table.column( 'html_fichier_fr:name' ).data();
        var html_mg = html_Table.column( 'html_fichier_mg:name' ).data();

        $('#txt_num_texte_mg').val(data_id[html_Index]);

        //alert(html_mg[html_Index]);

        //$('#body_modal_view_html_fr').html(html_fr[html_Index].replace(/http:\/\/localhost\/legisapplications\//g,$('#chemin_source').val()));
        $('#body_modal_view_html_mg').html(html_mg[html_Index].replace(/http:\/\/localhost\/legisapplications\//g,$('#chemin_source').val()));

        $('#confirm-view-html-mg').modal('show');
        $('#fermer_modal').show();

        $('#btnfermer_view_html').on('click',function(){
            $('#btnfermer_view_html').unbind();
            $('#View_html_mg').unbind();
        });
        //alert('View_html');
    });


});