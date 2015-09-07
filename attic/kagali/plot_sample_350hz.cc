void plot_sample_350hz(){
  TCanvas *c1 = new TCanvas("c1","Graph Draw Options",400,10,600,500);
  c1->Divide(1,2);
  c1->SetFillStyle(4100);
  c1->SetGrid();
  c1->GetFrame()->SetBorderSize(12);
  
  gStyle->SetOptStat(0);
  gStyle->SetLabelSize(0.07,"X");
  gStyle->SetLabelSize(0.07,"Y");
  gStyle->SetTitleOffset(0.7,"Y");
  
  float t1,t2,A,f,phi;
  float f1 = 334.0;
  float f2 = 340.0;
  float f3 = 350.0;
  
  stringstream cond;
  cond << "f>" << f1 << " && f<" << f3;
  
  stringstream cond1;
  cond1 << "f>" << f1 << " && f<" << f2;
  
  stringstream cond2;
  cond2 << "f>" << f2 << " && f<" << f3;
  
  TNtuple *nt = new TNtuple("nt","Demo ntuple","t1:t2:A:f:phi");
  ifstream data("LIGOtest.ana");
  int index=0;
  while (data >> t1 >> t2 >> A >> f >> phi) {
    nt->Fill(t1,t2,A,f,phi);
  }
  data.close();
  
  nt->SetMarkerStyle(7);
  c1->cd(1);
  nt->SetMarkerColor(1);
  nt->Draw("A:(t1+t2)/2>>htmp(20,0.0,10.0,20,0,1.0)",cond.str().c_str(),"");
  TH2F *h = gPad->GetListOfPrimitives()->FindObject("htmp");
  h->SetTitle("");
  h->GetXaxis()->CenterTitle();
  h->GetYaxis()->CenterTitle();
  h->SetXTitle("Time [sec]");
  h->SetYTitle("Amplitude");
  gPad->ls();
  gPad->Modified();
  
  nt->SetMarkerColor(4);
  nt->Draw("A:(t1+t2)/2",cond1.str().c_str(),"same");
  nt->SetMarkerColor(2);
  nt->Draw("A:(t1+t2)/2",cond2.str().c_str(),"same");
  
  c1->cd(2); 
  nt->SetMarkerColor(1);
  nt->Draw("f:(t1+t2)/2>>htmp(20,0,10.0,20,334,350)","","");
  TH2F *h2 = gPad->GetListOfPrimitives()->FindObject("htmp");
  h2->SetTitle("");
  h2->GetXaxis()->CenterTitle();
  h2->GetYaxis()->CenterTitle();
  h2->SetXTitle("Time [sec]");
  h2->SetYTitle("Frequency [Hz]");
  gPad->ls();
  gPad->Modified();
  
  nt->SetMarkerColor(4);
  nt->Draw("f:(t1+t2)/2",cond1.str().c_str(),"same");
  nt->SetMarkerColor(2);
  nt->Draw("f:(t1+t2)/2",cond2.str().c_str(),"same");
  
  c1->SaveAs("plot_sample_350hz.eps");
}
